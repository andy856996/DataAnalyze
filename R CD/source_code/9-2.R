# ---------
#  9-2
# ---------
# ---------------------------------------------------------- #

use.switch <- function(x)
{
   switch(x,
      "a"="first",
      "b"="second",
      "z"="last",
      "c"="third",
      "other")
}

use.switch("a")
use.switch("b")
use.switch("c")
use.switch("d")
use.switch("e")
use.switch("z")

# ---------------------------------------------------------- #

use.switch(1)
use.switch(2)
use.switch(3)
use.switch(4)
use.switch(5)
use.switch(6) # 沒回傳任何東西
is.null(use.switch(6))

# ---------------------------------------------------------- #