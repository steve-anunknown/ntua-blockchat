#!/bin/bash

BASE_IP="172.0.0."
BASE="0"
PORT="36900"
IMAGE_NAME="blockchat"
PREFIX="experiments/profiled_outputs/docker_harmonic"
SUFFIX=""
CAPACITY=("5" "10" "20" "1")
TESTS=("throughput" "scalability" "fairness")
PROFLOG="/app/node"

for TEST in "${TESTS[@]}";
do
    if [ "$TEST" != "scalability" ];
    then
        NODES="5"
    else
        NODES="10"
    fi
    if [ "$TEST" == "fairness" ];
    then
        initial_stake="stake 100"
    else
        initial_stake="stake 10"
    fi

    for CAP in "${CAPACITY[@]}";
    do
        WORKDIR=$PREFIX/$TEST$SUFFIX/capacity$CAP
        if [ ! -d "$WORKDIR" ];
        then
            mkdir -p "$WORKDIR"
        fi
        
        docker network create --subnet=172.0.0.0/16 blockchat-net
        echo "spawning bootstrap node"
        docker run \
            --net blockchat-net \
            --ip "$BASE_IP$((BASE+2))"\
            -p $PORT:$PORT \
            --name bootstrap\
            $IMAGE_NAME --bootstrap "$BASE_IP$((BASE+2))" $PORT $NODES &
    
        sleep 2

        docker logs -f bootstrap > "$WORKDIR"/bootstrap.log &

        echo "spawning node 1"
        input="experiments/input$NODES/trans1.txt"
        (cat <(echo "$initial_stake") <(echo "load $input") <(echo "blockchain") <(echo "balance"))|\
            docker run -i \
                --net blockchat-net \
                --ip "$BASE_IP$((BASE+2+1))"\
                -p $((PORT+1)):$PORT \
                --name node1\
                $IMAGE_NAME --node "$BASE_IP$((BASE+2+1))" "$PORT" "$BASE_IP$((BASE+2))" \
                "$PORT" "$CAP" &
        sleep 2
        docker logs -f node1 > "$WORKDIR"/node1.log &
        
        for i in $(seq 2 $NODES);
        do
            echo "spawning node ${i}"
            input="experiments/input$NODES/trans$i.txt"
            (cat <(echo "stake 10") <(echo "load $input") <(echo "blockchain") <(echo "balance"))|\
                docker run -i \
                    --net blockchat-net \
                    --ip "$BASE_IP$((BASE+2+i))"\
                    -p $((PORT+i)):$PORT \
                    --name node"$i"\
                    $IMAGE_NAME --node "$BASE_IP$((BASE+2+i))" "$PORT" "$BASE_IP$((BASE+2))" \
                    "$PORT" "$CAP" &
            sleep 2
            docker logs -f node"$i" > "$WORKDIR"/node"$i".log &
        done

        # wait for containers from node1 to node$NODES to finish
        for i in $(seq 1 $NODES);
        do
            echo "waiting for node $i to finish"
            docker wait node"$i"
            # get the profiled outputs
            docker cp node"$i":$PROFLOG.prof "$WORKDIR"/node"$i".prof
            docker rm node"$i"
        done
        docker rm bootstrap
        docker network rm blockchat-net
    done
done
