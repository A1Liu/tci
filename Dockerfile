FROM node:latest as build

WORKDIR /usr/src/app
ENV USER=tci

RUN apt-get update
RUN apt-get install -y curl build-essential
RUN apt-get install zlib1g-dev libtinfo-dev libxml2-dev -y

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup target add wasm32-unknown-unknown
RUN cargo install --color=never wasm-pack

COPY .cargo/ ./.cargo/
COPY lib/parcel-plugin-wasm-pack/ ./lib/parcel-plugin-wasm-pack/
COPY lib/codespan-reporting/ ./lib/codespan-reporting/

COPY package.json yarn.lock ./
RUN yarn install --pure-lockfile
RUN cargo init --color=never --lib

COPY Cargo.toml Cargo.lock ./
RUN cargo build --color=never --release --target wasm32-unknown-unknown
RUN rm ./.build/wasm32-unknown-unknown/release/deps/tci*

COPY lib/ ./lib/
COPY src/ ./src/
COPY web/ ./web/
RUN yarn build

FROM python:3-alpine
WORKDIR /usr/src/app
COPY --from=build /usr/src/app/.dist ./.dist/

EXPOSE 1234
ENTRYPOINT ["python3", "-m", "http.server", "--directory", ".dist", "1234"]
