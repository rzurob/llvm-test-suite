!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 11,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : support for structure constructors containing type parameters to the DATA statement.
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333315
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  type structure constructors containing type
!*  parameters to the Block DATA statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program exam
                type SimpleType(k1,k2,len1,len2)
                        integer,kind :: k1
                        integer,kind :: k2
                        integer,len  :: len1
                        integer,len  :: len2
                        sequence
                        logical*8 l8
                         logical   l
                        logical*4 l4
                        logical*2 l2
                        logical*1 l1
        end type SimpleType

        type (SimpleType(4,2,4,1)) dtCommon
        common /cblock/ dtCommon

        print *,dtCommon%k1,dtCommon%k2,dtCommon%len1,dtCommon%len2
        print *,dtCommon

        end

        block data
                type SimpleType(k1,k2,len1,len2)
                        integer,kind :: k1
                        integer,kind :: k2
                        integer,len  :: len1
                        integer,len  :: len2
                        sequence
                        logical*8 l8
                        logical   l
                        logical*4 l4
                        logical*2 l2
                        logical*1 l1
                end type SimpleType

                integer i
                type (SimpleType(4,2,4,1)) dtCommon
                common /cblock/ dtCommon

                data dtCommon/SimpleType(4,2,4,1)(.true.,.false.,.true.,.false.,.true.)/
        end

