FROM alpine

RUN apk add --update --no-cache nodejs npm
RUN npm i -g yarn
RUN apk --no-cache add build-base curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY codespan-reporting/ /app/codespan-reporting/
COPY embedded-websocket/ /app/embedded-websocket/
COPY frontend/package.json frontend/yarn.lock /app/frontend/
RUN cd /app/frontend && yarn install --pure-lockfile

COPY frontend/ /app/frontend/
RUN cd /app/frontend && yarn build

COPY Cargo.toml Cargo.lock /app/
COPY includes/ /app/includes/
COPY src/ /app/src/
RUN cd /app/ && ~/.cargo/bin/cargo install --path .

EXPOSE 3000
ENTRYPOINT ["/root/.cargo/bin/tci"]
