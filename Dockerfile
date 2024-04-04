
FROM haskell:9.6.4

WORKDIR /app

# Copy only the files needed for stack to download and install dependencies
COPY stack.yaml package.yaml /app/
RUN stack setup && stack build --dependencies-only

# Now copy the rest of the project
COPY . /app

# Reuse .stack-work if possible (consider this based on your CI/CD setup)
COPY .stack-work /app/.stack-work

# Build the project
RUN stack build --profile

ENTRYPOINT ["stack", "exec", "--profile", "--", "BlockChat-exe"]
CMD []
