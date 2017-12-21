!--------------------------------------------------------------
! OpenNetCDF:
!         Open a netcdf file, read the attributes and
!         array dimensions.
!--------------------------------------------------------------

      subroutine OpenContinum (ncid, name, nrhox, ntemp, nufreq, ier)

!
      implicit none
!
      include 'netcdf.inc'

      integer          ier

      character(80)    name
      integer::        ncid, nrhox, ntemp, nufreq
      integer::        status, varid
      integer::        nrhoxid, ntempid, nufreqid

      ier = -1
      ncid = -1
! NF_OPEN reads named file, NF_WRITE implies file is writeable
! ncid is returned NetCDF file ID.

      status =  NF_OPEN( name, NF_WRITE, ncid )
      if ( status .ne. NF_NOERR ) then
         print*,'ERROR: Unable to open file ', name
         call  handle_error(status)
         return
      end if

 

!.......Read the dimension id's
! nf_inq_dimid returns dimension ID (integer) of "string" in ncid
 
      status = nf_inq_dimid( ncid, 'nrhox', nrhoxid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimid( ncid, 'ntemp', ntempid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimid( ncid, 'nufreq', nufreqid )
      if ( status .ne. NF_NOERR ) call  handle_error(status)

!.......Read the actual dimensions 
! nf_inq_dimlen returns length of dimension
!  
      status = nf_inq_dimlen( ncid, nrhoxid, nrhox )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimlen( ncid, ntempid, ntemp )
      if ( status .ne. NF_NOERR ) call  handle_error(status)
      status = nf_inq_dimlen( ncid, nufreqid, nufreq )
      if ( status .ne. NF_NOERR ) call  handle_error(status)


      ier = 0

      return 

      end subroutine  OpenContinum 
