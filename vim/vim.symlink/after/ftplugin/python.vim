setlocal copyindent
setlocal formatprg=python3\ -m\ black\ --quiet\ -

if 0 != 0+search('import logging', 'nw', 0, 100)
  let b:printf_pattern = "logging.info('%{}'.format(%s))"
else
  let b:printf_pattern = "print('%{}'.format(%s))"
endif
