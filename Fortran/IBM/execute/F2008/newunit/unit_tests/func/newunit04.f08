       ! negative unit numbers obtained from NEWUNIT= are allowed in
       ! the OPEN statement when changing connection mode.
       ! Also tests newunit numbers with INQUIRE.

       integer unit_number
       integer i, j
       character(5) decimal_mode
       open(newunit=unit_number, status='scratch', decimal='comma')
       inquire(unit_number, decimal=decimal_mode)
       if (decimal_mode /= 'COMMA') then
         print *, decimal_mode
         stop 1
       endif

       write(unit_number, '(F3.1)') 1.5
       rewind(unit_number)

       ! change the decimal mode of the connection
       open(unit_number, decimal='point')
       inquire(unit_number, decimal=decimal_mode)
       if (decimal_mode /= 'POINT') then
         print *, decimal_mode
         stop 2
       endif

       read(unit_number, *) i, j
       close(unit_number)

       if (i /= 1) then
         print *, i
         stop 3
       endif

       if (j /= 5) then
         print *, j
         stop 4
       endif

       end
