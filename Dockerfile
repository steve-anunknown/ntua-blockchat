FROM haskell:9.6.4

WORKDIR /app
COPY . /app

# Now using the specified Stack version for setup and build
RUN stack setup
RUN stack build --profile

# more arguments will be passed to `stack exec`
ENTRYPOINT ["stack", "exec", "--profile", "--", "BlockChat-exe"]
# Optional: Default arguments that can be overridden
CMD []

