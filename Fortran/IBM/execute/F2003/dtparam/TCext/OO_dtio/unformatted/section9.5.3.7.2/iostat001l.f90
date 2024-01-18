! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : iostat001l
!*
!*  PROGRAMMER                 : David Forster (derived from iostat001 by Robert Ma)
!*  DATE                       : 2007-09-18 (original: 11/04/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: For iostat equals to non-zero, at runtime, the i/o procedure should stop 
!*                                        the execution of the program
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base (lbase_1) ! lbase_1=3
       integer, len :: lbase_1
       character(lbase_1) :: c
    end type
end module

program iostat001l
use m
    
   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
   
   ! declaration of variables
   
   integer :: stat
   character(30) :: msg
   
   class(base(:)), allocatable :: b1 ! tcx: (:)
   
   ! allocation of variables
   
   allocate (base(3):: b1 ) ! tcx: base(3)

   open(1, file='iostat001l.data', access='sequential', form='unformatted')
      
    write (1, err = 100)                b1
    print *, 'error'
100 print *, 'great!'
    write (1, iostat = stat )           b1 
    if ( stat /= 1 ) error stop 101_4
   
   ! close the file appropriately
   
   ! close ( 1, status ='delete' )
   
end program


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write ( unit, iostat= iostat, iomsg=iomsg )    dtv%c
   iostat = 1

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
