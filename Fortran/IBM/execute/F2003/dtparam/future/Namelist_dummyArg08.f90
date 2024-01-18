!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Namelist with Intrinsic IO
!*  SECONDARY SUBROUTINES TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : NAMELIST
!*
!*  DESCRIPTION                :
!*  namelist-stmt is
!*
!*     NAMELIST  / namelist-group-name / namelist-group-object-list &
!*     [ [ , ] / namelist-group-name / namelist-group-object-list ] . . .
!*
!*  see defec 359604 and 356679
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)  !(4,10)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1/k1) :: Iarr(l1) = k1
        COMPLEX(k1)    :: Zarr(l1/5)   = (k1,k1)
        CHARACTER(l1)  :: Carr(l1) = 'ABCD '
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)  !(4,10)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: dtarr(l2/l1)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
      END TYPE NextGen

      TYPE, EXTENDS(NextGen) :: LastGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
      END TYPE LastGen

      CONTAINS

      SUBROUTINE sub1(arg1, arg2, arg3)
        TYPE(Base(4,*))  :: arg1
        TYPE(LastGen(4,*,4,*,1024,*,1024,*)), OPTIONAL  :: arg2 , arg3(:)

       NAMELIST /NML1/arg1, /NML2/arg1,arg2, /NML3/arg1,arg3, /NML4/arg1,arg2,arg3

       OPEN (1, file = 'Namelist_dummyArg08.In', form='formatted')

      IF ( (.not. present (arg2)) .and. (.not. present(arg3)) ) THEN
         READ(1, NML=NML1)
         IF (ANY(arg1%Iarr .NE. 4)) STOP 10
         IF (ANY(arg1%Zarr .NE. (4,4))) STOP 11
         IF (ANY(arg1%Carr .NE. 'ABCD')) STOP 12

         WRITE(*, NML=NML1, DELIM='APOSTROPHE')

      ENDIF

      IF ( present (arg2) .and. (.not. present(arg3)) ) THEN
         READ(1, NML=NML2)
         IF (ANY(arg1%Iarr .NE. 4)) STOP 13
         IF (ANY(arg1%Zarr .NE. (4,4))) STOP 14
         IF (ANY(arg1%Carr .NE. 'ABCD')) STOP 15

         IF (ANY(arg2%Iarr .NE. 1)) STOP 16
         IF (ANY(arg2%Zarr .NE. (9.5,9.5))) STOP 17
         IF (ANY(arg2%Carr .NE. 'Niels')) STOP 18
         IF (ANY(arg2%dtarr(1)%Iarr .NE. 6)) STOP 19
         IF (ANY(arg2%dtarr(1)%Zarr .NE. (6.6,6.6))) STOP 20
         IF (ANY(arg2%dtarr(1)%Carr .NE. 'Erwin')) STOP 21

         WRITE(*, NML=NML2, DELIM='APOSTROPHE')
      ENDIF

      IF ( present (arg3) .and. (.not. present(arg2)) ) THEN
         STOP 100
      ENDIF

      IF ( present (arg2) .and. present(arg3) ) THEN
         READ(1, NML=NML4)
         IF (ANY(arg1%Iarr .NE. 4)) STOP 31
         IF (ANY(arg1%Zarr .NE. (4,4))) STOP 32
         IF (ANY(arg1%Carr .NE. 'ABCD ')) STOP 33

         IF (ANY(arg2%Iarr .NE. 1)) STOP 34
         IF (ANY(arg2%Zarr .NE. (9.5,9.5))) STOP 35
         IF (ANY(arg2%Carr .NE. 'Niels')) STOP 36
         IF (ANY(arg2%dtarr(1)%Iarr .NE. 6)) STOP 37
         IF (ANY(arg2%dtarr(1)%Zarr .NE. (6.6,6.6))) STOP 38
         IF (ANY(arg2%dtarr(1)%Carr .NE. 'Erwin')) STOP 39

         IF (ANY(arg3(1)%Iarr .NE. 7)) STOP 40
         IF (ANY(arg3(1)%Zarr .NE. (7.2,7.2))) STOP 41
         IF (ANY(arg3(1)%Carr .NE. 'Bohr')) STOP 42
         IF (ANY(arg3(1)%dtarr(1)%Iarr .NE. 8)) STOP 43
         IF (ANY(arg3(1)%dtarr(1)%Zarr .NE. (8.8,8.8))) STOP 44
         IF (ANY(arg3(1)%dtarr(1)%Carr .NE. 'Shrodinger')) STOP 45
         IF (ANY(arg3(2)%Iarr .NE. 7)) STOP 46
         IF (ANY(arg3(2)%Zarr .NE. (7.2,7.2))) STOP 47
         IF (ANY(arg3(2)%Carr .NE. 'Bohr')) STOP 48
         IF (ANY(arg3(2)%dtarr(1)%Iarr .NE. 8)) STOP 50
         IF (ANY(arg3(2)%dtarr(1)%Zarr .NE. (8.8,8.8))) STOP 51
         IF (ANY(arg3(2)%dtarr(1)%Carr .NE. 'Shrodinger')) STOP 52

         WRITE(*, NML=NML4, DELIM='APOSTROPHE')
      ENDIF

      END SUBROUTINE sub1

END MODULE Mod1
!*
PROGRAM Namelist_dummyArg08
      USE MOD1
      IMPLICIT NONE

       CLASS(Base(4,:)), ALLOCATABLE :: d1, d2, d3(:)

       ALLOCATE(Base(4,10):: d1 )
       ALLOCATE(LastGen(4,10,4,10,1024,1024,1024,1024):: d2, d3(2) )

       call sub1(d1)

       SELECT TYPE(d2)
         TYPE IS (LastGen (4,*,4,*,1024,*,1024,*))

            call sub1(d1,d2)

            SELECT TYPE(d3)
              TYPE IS (LastGen (4,*,4,*,1024,*,1024,*))

                  call sub1(d1,d2,d3)

              CLASS DEFAULT
                STOP 11
            END SELECT

         CLASS DEFAULT
           STOP 10
       END SELECT

END PROGRAM Namelist_dummyArg08
