docker-build:
	docker build -t lucinda .

docker-push:
	docker tag lucinda:latest 096202052535.dkr.ecr.us-west-2.amazonaws.com/lucinda:latest
	docker push 096202052535.dkr.ecr.us-west-2.amazonaws.com/lucinda:latest

devtunnel:
  # TODO(igm): find a cleaner way of doing this

  # Vulgate
	ssh -i ~/legends_aws.pem -fNL 6205:localhost:6205 ubuntu@dev.asuna.io
  # Cassandra
	ssh -i ~/legends_aws.pem -fNL 9042:localhost:9042 ubuntu@dev.asuna.io

run:
	java -jar target/scala-2.11/lucinda-assembly.jar \
		--vulgate_host=localhost --vulgate_port=6205 \
		--redis_host=localhost --cassandra_hosts=localhost
