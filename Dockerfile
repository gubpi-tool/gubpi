FROM docker.io/library/ubuntu:20.04

RUN apt update

# Install the dotnet SDK
RUN apt install -y wget
RUN wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
RUN dpkg -i packages-microsoft-prod.deb && rm packages-microsoft-prod.deb

RUN apt-get update; \
  apt-get install -y apt-transport-https && \
  apt-get update && \
  apt-get install -y dotnet-sdk-6.0

# Install GCC and Make
RUN apt install -y build-essential

# Copy source files
COPY ./vinci ./vinci

COPY ./src ./src

# Build Vinci

WORKDIR  vinci

RUN make

# Build GuBPI

WORKDIR ./../src

RUN dotnet restore
RUN dotnet build -c release -o ../app --no-restore

# Copy the vinci exeutable to the app folder

WORKDIR ./..

RUN cp ./vinci/vinci ./app

# The entry point is the GuBPI executable
ENTRYPOINT ["/app/GuBPI"]

VOLUME ["/benchmarks"]
VOLUME ["/output"]
