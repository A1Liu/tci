FROM node:latest as react-build
WORKDIR /usr/src/app
COPY frontend/package.json frontend/yarn.lock ./
RUN yarn install --pure-lockfile
COPY frontend/postcss.config.js ./
COPY frontend/public/ ./public/
COPY frontend/src/ ./src/
RUN yarn build

FROM rust:1.47-alpine3.12 as cargo-build
RUN apk add --no-cache musl-dev

WORKDIR /usr/src/app
COPY --from=react-build /usr/src/app/build ./frontend/build/

ENV USER tci
RUN cargo init
COPY codespan-reporting/ ./codespan-reporting/
COPY embedded-websocket/ ./embedded-websocket/
COPY Cargo.toml Cargo.lock ./
RUN cargo build --release

COPY includes/ ./includes/
COPY src/ ./src/
RUN rm ./target/release/deps/tci*
RUN cargo build --release

FROM alpine:3.12
COPY --from=cargo-build /usr/src/app/target/release/tci /bin/tci

EXPOSE 3000
ENTRYPOINT ["/bin/tci"]
