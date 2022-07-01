FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://gitlab.com/sosy-lab/sv-comp/archives-2022/raw/svcomp22/2022/symbiotic.zip
RUN apt update && apt install -y zip
RUN unzip symbiotic.zip

RUN apt-get install -y python3
RUN apt-get install -y gcc-multilib

ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp
ADD witnesses.py /symbiotic/lib/symbioticpy/symbiotic/witnesses/witnesses.py
#
ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]