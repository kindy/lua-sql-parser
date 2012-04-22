
local parse = require 'sql_parser'.parse

local sql_ast = parse[[select * from a]]
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

