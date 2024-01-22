# sha digest because "stack build" takes forever
FROM haskell:9.8.1-slim@sha256:49ed7c437b14c526f7f5c3cd65924951751adf336202d9bfcfe993f1bd567ae8

WORKDIR /usr/src/app
COPY exercises/stack.yaml* /usr/src/app
COPY exercises/tests.cabal /usr/src/app
RUN stack build

COPY exercises /usr/src/app

# modify next line to change the wanted exercise
CMD ["stack", "runhaskell", "Set3bTest.hs"]