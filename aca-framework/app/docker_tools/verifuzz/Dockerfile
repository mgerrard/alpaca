FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://gitlab.com/sosy-lab/sv-comp/archives-2022/raw/svcomp22/2022/verifuzz.zip
RUN apt update && apt install -y zip
RUN unzip verifuzz.zip

RUN apt-get install -y default-jre
RUN apt-get install -y gcc-multilib
RUN apt-get install -y python2

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
#
ADD launch.sh /verifuzz/launch.sh
WORKDIR /verifuzz
ENTRYPOINT ["sh", "/verifuzz/launch.sh"]