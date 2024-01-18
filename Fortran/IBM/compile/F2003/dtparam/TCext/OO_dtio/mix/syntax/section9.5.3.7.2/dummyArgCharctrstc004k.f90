! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgCharctrstc004k
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArgCharctrstc004 by Robert Ma)
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics 
!*                                  -  different shape
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base1 (kbase1_1) ! kbase1_1=4
      integer, kind :: kbase1_1
      integer(kbase1_1) :: i   
   end type
   
end module

program dummyArgCharctrstc004k
   use m1
   
   interface write(formatted)
      ! write : defines array of multiple rank instead of rank one
      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1(4)), intent(in) :: dtv(1)                  !<- shall be a scalar instead of rank-one array ! tcx: (4)
         integer, intent(in), dimension(1) :: unit           !<- shall be a scalar instead of rank-one array
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:,:)                  !<- shall be assumed shape array of rank one 
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
   
    interface read(formatted)
       ! read : defines array of multiple rank instead of rank one
       subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
          import base1
          class(base1(4)), intent(inout) :: dtv ! tcx: (4)
          integer, intent(in) :: unit
          character(*), intent(in), dimension(:) :: iotype   !<- shall be scalar
          integer, intent(in) :: v_list(*)                   !<- shall be assumed shape array of rank one
          integer, intent(out) :: iostat
          character(*), intent(inout), dimension(1) :: iomsg !<- shall be scalar instead of rank-one array
       end subroutine
   end interface
   
end program







! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 2 changes
