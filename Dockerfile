FROM rust:alpine AS builder

WORKDIR /usr/src/app
RUN apk add --no-cache build-base

COPY Cargo.toml Cargo.lock ./
RUN mkdir src && echo "fn main() {}" > src/main.rs
RUN cargo build --release && rm -rf src 

COPY . .
RUN cargo build --release

FROM alpine:latest

COPY --from=builder /usr/src/app/target/release/mdq .

RUN chmod +x mdq

ENTRYPOINT ["./mdq"]
