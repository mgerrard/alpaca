FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10198805/files/esbmc-kind-6.zip
RUN apt update && apt install -y zip
RUN unzip esbmc-kind-6.zip

RUN apt-get install -y python3
RUN apt-get install -y gcc-multilib
RUN mv esbmc-kind esbmc

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
#
ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]