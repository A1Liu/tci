FROM node:latest as react-build
WORKDIR /usr/src/app
COPY frontend/package.json frontend/yarn.lock ./
RUN yarn install --pure-lockfile
COPY frontend/postcss.config.js ./
COPY frontend/public/ ./public/
COPY frontend/src/ ./src/
RUN yarn build

FROM silkeh/clang:11 as cargo-build

RUN apt-get update
RUN apt-get install -y curl build-essential

WORKDIR /usr/src/app

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN apt-get install zlib1g-dev libtinfo-dev libxml2-dev -y

ENV USER=tci
RUN $HOME/.cargo/bin/cargo init

COPY codespan-reporting/ ./codespan-reporting/
COPY embedded-websocket/ ./embedded-websocket/
COPY Cargo.toml Cargo.lock ./
RUN $HOME/.cargo/bin/cargo build --release

COPY --from=react-build /usr/src/app/build ./frontend/build/
COPY includes/ ./includes/
COPY libs/ ./libs/
COPY src/ ./src/

RUN rm ./target/release/deps/tci*
RUN $HOME/.cargo/bin/cargo build --release


FROM silkeh/clang:11
COPY --from=cargo-build /usr/src/app/target/release/tci /bin/tci

EXPOSE 4000
ENTRYPOINT ["/bin/tci"]
