FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://zenodo.org/records/10243500/files/veriabs.zip
RUN apt update && apt install -y zip
RUN unzip veriabs.zip

RUN apt-get install -y default-jre
RUN apt-get install -y perl
RUN apt-get install -y gcc-multilib
ENV TZ=Asia/Dubai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get install -y python3-pandas
RUN mv VeriAbs veriabs

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp

ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]