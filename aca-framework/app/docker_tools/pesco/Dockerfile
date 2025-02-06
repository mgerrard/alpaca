FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10203346/files/pesco.zip
RUN apt update && apt install -y zip
RUN unzip pesco.zip

RUN apt-get install -y default-jre

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
ADD launch.sh /launch.sh
#ENTRYPOINT ["sh", "/launch.sh"]