FROM erlang:18

COPY _build/prod/rel/preminder /preminder
RUN ln -s /preminder/bin/preminder /bin/preminder

CMD ["preminder", "console"]
