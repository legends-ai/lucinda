cassandratunnel:
	lsof -twni tcp:9042 | xargs kill -9
	ssh -i ~/legends_aws.pem -fNL 9042:localhost:9042 ubuntu@dev.asuna.io

vulgatetunnel:
	lsof -twni tcp:6205 | xargs kill -9
	ssh -i ~/legends_aws.pem -fNL 6205:localhost:6205 ubuntu@dev.asuna.io

# TODO(igm): find a cleaner way of doing this
devtunnel: cassandratunnel vulgatetunnel

run:
	java -jar target/scala-2.11/lucinda-assembly.jar \
		--vulgate_host=localhost --vulgate_port=6205 \
		--redis_host=localhost --cassandra_hosts=localhost
