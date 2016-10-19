docker-build:
	docker build -t lucinda .

docker-push:
	docker tag lucinda:latest 096202052535.dkr.ecr.us-west-2.amazonaws.com/lucinda:latest
	docker push 096202052535.dkr.ecr.us-west-2.amazonaws.com/lucinda:latest
