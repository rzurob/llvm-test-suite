!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE and OPEN
!*  stmt with format declarations for E edit descriptor and printing to a file 
!*  with stream access. Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign051

    complex :: q = (1.0,1.0)
    complex(4) :: q4 = (1.0,1.0)
    complex(8) :: q8 = (1.0d0,1.0d0)
    complex(16) :: q16 = (1.0q0,1.0q0)

    open (1,file="iosign051.1",form='formatted',access='stream', &
          sign='processor_defined')

    write (1,*) "\nTest 1\n"

10  format(A4,2(X,E10.4))
11  format(A4,2(X,E12.5))
12  format(A4,2(X,E14.6))

    write (1,10) "q=", q
    write (1,10,sign='processor_defined') "q4=", q4
    write (1,11) "q8=", q8
    write (1,12,sign='processor_defined') "q16=", q16

    write (1,10) "q=", q
    write (1,10,sign='plus') "q4=", q4
    write (1,11) "q8=", q8
    write (1,12,sign='plus') "q16=", q16

    write (1,10) "q=", q
    write (1,10,sign='suppress') "q4=", q4
    write (1,11) "q8=", q8
    write (1,12,sign='suppress') "q16=", q16

    close(1)

    open (1,file="iosign051.1",form='formatted',position='append',&
          access='stream',sign='suppress')

    write (1,*) "\nTest 2\n"

13  format(A4,2(X,E10.4))
14  format(A4,2(X,E12.5))
15  format(A4,2(X,E14.6))

16  format(A4,2(X,E10.4))
17  format(A4,2(X,E12.5))
18  format(A4,2(X,E14.6))

    write (1,13) "q=", q
    write (1,13,sign='processor_defined') "q4=", q4
    write (1,14) "q8=", q8
    write (1,15,sign='processor_defined') "q16=", q16

    write (1,16) "q=", q
    write (1,16,sign='plus') "q4=", q4
    write (1,17) "q8=", q8
    write (1,18,sign='plus') "q16=", q16

    write (1,13) "q=", q
    write (1,13,sign='suppress') "q4=", q4
    write (1,14) "q8=", q8
    write (1,15,sign='suppress') "q16=", q16

    close(1)

    open (1,file="iosign051.1",form='formatted',position='append',&
          access='stream',sign='plus')

    write (1,*) "\nTest 3\n"

19  format(A4,2(X,E10.4))
20  format(A4,2(X,E12.5))
21  format(A4,2(X,E14.6))

    write (1,19) "q=", q
    write (1,19,sign='processor_defined') "q4=", q4
    write (1,20) "q8=", q8
    write (1,21,sign='processor_defined') "q16=", q16

    write (1,19) "q=", q
    write (1,19,sign='plus') "q4=", q4
    write (1,20) "q8=", q8
    write (1,21,sign='plus') "q16=", q16

    write (1,19) "q=", q
    write (1,19,sign='suppress') "q4=", q4
    write (1,20) "q8=", q8
    write (1,21,sign='suppress') "q16=", q16

    close(1)

end program iosign051

