FROM ubuntu:22.04

RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10203545/files/uautomizer.zip
RUN apt update && apt install -y zip
RUN unzip uautomizer.zip

RUN apt-get install -y python3
RUN apt-get install -y default-jre

RUN mv UAutomizer-linux ua
ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp

ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]