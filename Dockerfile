FROM rust:latest AS builder

WORKDIR /usr/src/app

COPY Cargo.toml Cargo.lock ./
RUN mkdir src && echo "fn main() {}" > src/main.rs
RUN cargo build --release && rm -rf src 

COPY . .
RUN cargo build --release

FROM debian:bookworm-slim

COPY --from=builder /usr/src/app/target/release/mdq /usr/local/bin/mdq

ENTRYPOINT ["mdq"]
