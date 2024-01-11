FROM haskell:9.8.1-slim

WORKDIR /usr/src/app
COPY exercises/stack.yaml* /usr/src/app
COPY exercises/tests.cabal /usr/src/app
RUN stack build

COPY exercises /usr/src/app

# modify next line to change the wanted exercise
CMD ["stack", "runhaskell", "Set3aTest.hs"]