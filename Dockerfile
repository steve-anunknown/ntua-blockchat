
FROM haskell:9.6.4

WORKDIR /app

# Copy the necessary files and directories for the build
COPY stack.yaml package.yaml /app/

# Install dependencies
RUN stack setup && stack build --dependencies-only

# Now copy the rest of the project
COPY . /app

# Reuse .stack-work if possible (consider this based on your CI/CD setup)
COPY .stack-work /app/.stack-work

# Build the project
RUN stack build

ENTRYPOINT ["stack", "exec", "--", "BlockChat-exe"]
CMD []
