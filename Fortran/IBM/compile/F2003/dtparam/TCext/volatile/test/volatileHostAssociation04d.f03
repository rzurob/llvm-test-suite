! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/volatile/test/volatileHostAssociation04d.f
! opt variations: -qck -qnok

!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Host Association, VOLATILE
!*  DESCRIPTION                :
!*        16.4.1.3
!* An internal subprogram, a module subprogram, or a derived-type
!* definition has access to the named entities from its host via
!* host association. An interface body has access via host association
!* to the named entities from its host that are made accessible by IMPORT
!* statements in the interface body. The accessed entities are known by
!* the same name and have the same attributes as in the host, except that
!* an accessed entity may have the VOLATILE or ASYNCHRONOUS attribute
!* even if the host entity does not.
!* ===================================================================

   module m
     type dt(k1,n1)    ! (4,4)
        integer, kind :: k1
        integer, len  :: n1
        character(n1) :: x
     end type

     type(dt(4,4)) :: d
     contains
           subroutine sub()
               VOLATILE :: d
               print *, d%x
           end subroutine sub
   end module m

   program volatileHostAssociation04d

     use m
     d%x="IBM"
     call sub()

   end program volatileHostAssociation04d

