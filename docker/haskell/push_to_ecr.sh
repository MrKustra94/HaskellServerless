#!/bin/bash

VERSION=${1:-"1.0.0_18.22"}
DOCKER_IMAGE="$AWS_REGISTRY/haskell-lambda:$VERSION"

aws ecr-public get-login-password --region "$AWS_ECR_REGION" | docker login --username AWS --password-stdin "$AWS_REGISTRY"
docker build -t "$DOCKER_IMAGE" .
docker push "$DOCKER_IMAGE"