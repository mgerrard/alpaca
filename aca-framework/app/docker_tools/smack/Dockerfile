FROM ubuntu:18.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://gitlab.com/sosy-lab/sv-comp/archives-2022/raw/svcomp22/2022/smack.zip
RUN apt update && apt install -y zip
RUN unzip smack.zip

RUN apt-get install -y python3

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
#
ADD launch.sh /launch.sh
#ENTRYPOINT ["sh", "/launch.sh"]