FROM ubuntu:22.04

# install wget
RUN apt update && apt install -y wget
RUN wget https://gitlab.com/sosy-lab/sv-comp/archives-2022/raw/svcomp22/2022/cpachecker.zip
RUN apt update && apt install -y zip
RUN unzip cpachecker.zip

RUN apt-get install -y default-jre
RUN mv CPAchecker-2.1-unix cpa
ADD PropertyUnreachCall.prp /PropertyUnreachCall.prp

ADD launch.sh /launch.sh
ENTRYPOINT ["sh", "/launch.sh"]