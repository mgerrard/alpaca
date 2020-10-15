FROM ubuntu:18.04

# Install benchexec
RUN apt update && apt install -y wget
RUN wget https://github.com/sosy-lab/benchexec/releases/download/2.7/benchexec_2.7-1_all.deb
RUN apt install -y --install-recommends ./benchexec_*.deb
RUN adduser root benchexec

COPY ./download_tools.sh /
COPY ./PropertyUnreachCall.prp /
#ENTRYPOINT ["/file.sh"]
RUN mkdir /home/alpaca_logs
RUN apt update && apt install -y emacs

RUN ln -s /usr/bin/python2.7 /usr/bin/python
RUN apt update && apt install -y python2.7
RUN apt update && apt install -y libc6-dev-i386
RUN apt update && apt install -y python-pycparser
RUN apt update && apt install -y libgmp-dev
RUN apt update && apt install -y zip

RUN /download_tools.sh
COPY ./run_benchexec.sh /
RUN apt update && apt install -y openjdk-8-jre
ENTRYPOINT ["/run_benchexec.sh"]
