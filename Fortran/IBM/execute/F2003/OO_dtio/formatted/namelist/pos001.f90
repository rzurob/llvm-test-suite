!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: namelist formatting with pos= specifier
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program pos001

   character(2) :: c = 'Aa'
   namelist /nml/ c

   open ( 1, file = 'pos001.1', form='formatted', access='stream' )
   write (1,nml, pos=4, delim='quote')
   c = 'xx'
   read (1, nml, pos=4)
   if ( c /= 'Aa' ) error stop 1_4

   close (1, status='delete')
end program

