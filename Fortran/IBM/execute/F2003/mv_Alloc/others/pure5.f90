! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : pure5.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : move_alloc is called in a pure function 
!*                               pure function is called inside forall stmt 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

          module m
              contains
                  pure integer function fun(i)
                       integer, intent(in) ::  i
                       allocatable fun, j

                       allocate(j, source= -i)
                       call move_alloc(j, fun) 

                  end function 
          end module
            
                use m

                integer i, a(:)
                allocatable a

                allocate(a(-4:5), source = (/ (i, i=-4,5) /) )

                forall (i=-4:5, a(i) > 0 )
                    a(i) = fun(i )
                end forall

                print *, a
                end
