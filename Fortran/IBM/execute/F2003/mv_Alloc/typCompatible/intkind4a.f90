! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : intkind4a.f 
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
!*  DESCRIPTION                : FROM is of type integer 
!*                               TO is of unlimited poly
!*                               multipul layers of select type to get TO
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type  :: base
          class(*), allocatable ::  i1 
      end type 

      type, extends(base) :: child
          integer, allocatable ::   i2
      end type

      class(base), allocatable :: b

      allocate(child :: b)

      select type ( b)
         type is (child)
            allocate(b%i2, source = 101 ) 
            allocate( base :: b%i1)
      
            select type ( x => b%i1)
               type is ( base)
                  allocate( child :: x%i1 )
                  select type ( y => x%i1 )
                     type is (child) 
                        allocate ( y%i1, source = 99 )
                        call move_alloc(b%i2, y%i1)

			if ( .not. allocated(y%i1) ) stop 22

                        select type (z => y%i1)
                           type is (integer)
                              if ( z /= 101 ) stop 23
                           class default 
                              stop 25
                        end select
                     class default
                        stop 31
                  end select 
               class default
                  stop 41
            end select
            
            if ( allocated (b%i2) ) stop 31 
         class default
            stop 51
      end select
end
