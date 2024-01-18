! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/volatile/test/volatileHostAssociation01d.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Host Association, VOLATILE
!*  DESCRIPTION                :
!*        16.4.1.3
!*                      An interface body has access via host association
!* to the named entities from its host that are made accessible by IMPORT
!* statements in the interface body. The accessed entities are known by
!* the same name and have the same attributes as in the host, except that
!* an accessed entity may have the VOLATILE or ASYNCHRONOUS attribute
!* even if the host entity does not.
!* ===================================================================

   program volatileHostAssociation01d

     type dt(k1)    ! (4)
        integer, kind :: k1
       sequence
        integer(k1)   :: x
        real(k1)      :: y
     end type
     type(dt(4)) :: b
     byte     :: a
     interface
        subroutine sub(c)
           import dt
           import b, a
           type (dt(4)) c
           VOLATILE b
           VOLATILE a
         end subroutine sub
      end interface

   end program volatileHostAssociation01d

