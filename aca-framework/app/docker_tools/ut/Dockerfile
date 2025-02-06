FROM ubuntu:22.04

RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10203547/files/utaipan.zip
RUN apt update && apt install -y zip
RUN unzip utaipan.zip

RUN apt-get install -y python3
RUN apt-get install -y default-jre

RUN mv UTaipan-linux ut
ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp

ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]