FROM java:latest

# Copy files. we don't need glide etc with this
COPY ./target/scala-2.11/lucinda-assembly-*.jar /app
WORKDIR /app
