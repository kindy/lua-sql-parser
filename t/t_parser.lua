parse = require 'sql_parser'.parse
cjson = require 'cjson'

sql_ast = parse[[
    (1 + 2) * (3 - 4) + 5;
]]

print(cjson.encode(sql_ast))
-- {
--  type = 'select',
--  value = '',
--  sub = {
--      {
--        type = 'select_arg',
--        value = '*',
--      },
--      {
--        type = 'from',
--        value = 'a',
--      },
--  }
-- }


