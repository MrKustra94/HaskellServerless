ARG AWS_REGISTRY

######DEPENDENCIES######
FROM ${AWS_REGISTRY}/haskell-lambda:1.0.0_18.22 as build

COPY stack.yaml package.yaml /root/lambda-function/
WORKDIR /root/lambda-function/

ARG GHC_OPTS="-j6 +RTS -A1024m -n2m -RTS"
RUN stack build --ghc-options "${GHC_OPTS}" --system-ghc --dependencies-only

COPY . /root/lambda-function/
RUN stack build --system-ghc --ghc-options "${GHC_OPTS}"

RUN mkdir -p /root/output && mkdir -p /root/output/lib
RUN cp -R $(stack path --local-install-root)/bin /root/output/
ENTRYPOINT sh

######DEPLOY######
FROM public.ecr.aws/lambda/provided:al2 as deploy
ARG EXECUTABLE_NAME

WORKDIR ${LAMBDA_RUNTIME_DIR}
COPY --from=build /root/output/bin .
RUN mv ${EXECUTABLE_NAME} bootstrap || true
CMD [ "handler" ]