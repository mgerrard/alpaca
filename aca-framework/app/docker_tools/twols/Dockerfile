FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10184626/files/2ls.zip
RUN apt update && apt install -y zip
RUN unzip 2ls.zip

RUN apt-get install -y gcc-multilib

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]