FROM ghcr.io/chris-ch/docker-haskell-aws-lambda:v1.3.0

# executed every time the container starts
COPY docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]

CMD [ "sleep", "infinity" ]
