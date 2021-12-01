
FROM haskell:buster

RUN apt update && apt install -y wget fontconfig unzip zsh vim python3
# install specific go version
RUN wget -c https://dl.google.com/go/go1.15.6.linux-amd64.tar.gz -O - | tar -xz -C /usr/local
RUN export PATH=$PATH:/usr/local/go/bin

# install my preferred font for development
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"

# create user for VSC so that we are not running as root
RUN useradd -ms /bin/zsh vscodeuser
USER vscodeuser
WORKDIR /home/vscodeuser

# install oh-my-zsh
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# configure necessary Go directories and binaries to be found in the PATH
RUN export GOPATH=/home/vscodeuser/go
RUN echo 'PATH=$PATH:/usr/local/go/bin' >> ~/.zshrc

# configure oh-my-zsh theme
RUN sed -i 's/ZSH_THEME.*/ZSH_THEME="Soliah"/' ~/.zshrc
RUN mkdir ~/.ghc/ && touch ~/.ghc/ghci.conf

# Configure increased level of warnings for Haskell compiler
RUN echo ':set -Wall' >> ~/.ghc/ghci.conf
