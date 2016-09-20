FROM erlang:18

ADD https://github.com/Yelp/dumb-init/releases/download/v1.1.2/dumb-init_1.1.2_amd64 /bin/dumb-init
RUN chmod +x /bin/dumb-init

COPY _build/prod/rel/preminder /preminder
RUN ln -s /preminder/bin/preminder /bin/preminder

## SIGTERM to SIGKILL
ENTRYPOINT ["dumb-init", "-c", "-r", "15:9", "--"]
CMD ["preminder", "console"]
