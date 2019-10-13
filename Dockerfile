FROM haskell:8.2.2

WORKDIR /app

RUN apt-get update && \
    apt-get install -y libmagic-dev

COPY . .
RUN stack setup
RUN stack build

EXPOSE 3000

CMD ["stack", "exec", "rh-server"]
