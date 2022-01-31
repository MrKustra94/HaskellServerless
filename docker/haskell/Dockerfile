FROM public.ecr.aws/lambda/provided:al2 as dependencies

ARG STACK_RESOLVER=lts-18.22

USER root
# Saving default system libraries before doing anything else
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries
# Installing basic dependencies
RUN yum install -y \
    git-core \
    tar \
    sudo \
    xz \
    make \
    gmp-devel \
    postgresql-devel \
    libicu libicu-devel \
    libyaml libyaml-devel

RUN yum groupinstall -y "Development Tools" "Development Libraries"
RUN curl -ksSL https://get.haskellstack.org/ | sh
RUN stack setup --resolver=${STACK_RESOLVER}

# # Installing common packages so that docker builds are faster
COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install \
    --ghc-options "-j6 +RTS -A256m -n2m -RTS" \
    --resolver=${STACK_RESOLVER} aeson text bytestring async string-conv safe \
    safe-exceptions-checked \
    aws-lambda-haskell-runtime-4.1.0 \
    unliftio-0.1.2.0 \
    amazonka-1.6.1 \
    amazonka-core-1.6.1 \
    amazonka-dynamodb-1.6.1 \
    fast-logger-3.1.1 \
    binary-0.8.8.0

RUN mkdir /root/lambda-function
