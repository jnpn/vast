clean:
	find . -type f -iname '*~' -delete
	find . -type f -iname '#*' -delete

test:
	nosetests -v
