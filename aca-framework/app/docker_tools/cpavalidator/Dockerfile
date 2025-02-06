FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10203297/files/CPAchecker-2.3-unix.zip
RUN apt update && apt install -y zip
RUN unzip CPAchecker-2.3-unix.zip

RUN apt-get install -y openjdk-17-jdk-headless
RUN mv CPAchecker-2.3-unix cpa
ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp

ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]