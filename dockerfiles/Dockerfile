FROM reg_shiny_base2:latest 

ARG LOCAL_USER_ID 
ENV LOCAL_USER_ID $LOCAL_USER_ID
ENV APP_HOME "/home/bdb"

RUN useradd -m -d $APP_HOME bdb \
	&& chown -R bdb:bdb $APP_HOME \
	&& echo bdb:bdb | chpasswd

# set up git ssh
USER bdb
RUN mkdir -p $APP_HOME/.ssh/
COPY azdevops $APP_HOME/.ssh/id_rsa
RUN ssh-keyscan ssh.dev.azure.com >> $APP_HOME/.ssh/known_hosts

# git clone shiny app
RUN mkdir $APP_HOME/app \
    && git clone git@ssh.dev.azure.com:v3/BDBiosciences/AB-Tech-Dev/stability-regression-shiny-app $APP_HOME/app

# change ownership to UNAME
USER root
RUN chown -R $UNAME:$UNAME $APP_HOME

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/bdb/app', host= '0.0.0.0', port = 3838)"]
