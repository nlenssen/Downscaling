%
%
%

ddir = '/ptmp/thoar/downscaling';
ddir = '/ptmp/thoar/downscaling/done';

fnames = {'tas_A1.20C3M_1.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_2.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_3.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_4.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_5.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_6.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_7.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_8.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.20C3M_9.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'tas_A1.Commit_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.Commit_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.Commit_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.Commit_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.Commit_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.Commit_6.CCSM.atmm.2000-01_cat_2049-12.downscaled.nc',
          'tas_A1.Commit_7.CCSM.atmm.2000-01_cat_2049-12.downscaled.nc',
          'tas_A1.Commit_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_6.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_7.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA1B_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_6.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_7.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_8.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESB1_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'tas_A1.SRESA2_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc'};


fnames = {'ppt_A1.20C3M_1.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_2.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_3.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_4.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_5.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_6.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_7.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_9.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.20C3M_EA1-5.CCSM.atmm.1870-01_cat_1999-12.downscaled.nc',
          'ppt_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_6.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_7.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA1B_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.Commit_6.CCSM.atmm.2000-01_cat_2049-12.downscaled.nc',
          'ppt_A1.Commit_7.CCSM.atmm.2000-01_cat_2049-12.downscaled.nc',
          'ppt_A1.Commit_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESA2_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_1.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_2.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_3.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_4.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_6.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_7.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_8.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc',
          'ppt_A1.SRESB1_EA1-5.CCSM.atmm.2000-01_cat_2099-12.downscaled.nc'};

fnames = {'tas_A1.PDcntrl_1.CCSM.atmm.0100-01_cat_0409-12.downscaled.nc',
          'tas_A1.PIcntrl_1.CCSM.atmm.0280-01_cat_0509-12.downscaled.nc',
          'tas_A1.PIcntrl_2.CCSM.atmm.0300-01_cat_0799-12.downscaled.nc'};

fnames = {'tas_A1.PIcntrl_2.CCSM.atmm.0300-01_cat_0799-12.downscaled.nc'};

clf
orient landscape

for  ifile = 1:length(fnames)

   str1 = fnames{ifile};
   experiment = str1(1:(findstr(str1,'.nc')-1));

   fname = sprintf('%s/%s',ddir,fnames{ifile});
   disp(sprintf('working on %s',fnames{ifile}))

   lons  = getnc(fname,'lon');
   lats  = getnc(fname,'lat');
   times = getnc(fname,'time') + datenum(1601,1,1);
   
   start = [ 1,417,1320];
   endpt = [-1,419,1322];
   
   disp('getting cape cod region ...')
   bob = getnc(fname,'tas',start,endpt);
   ted = squeeze(sum(bob,1));
   sam = [ted(1,2) ted(1,3) ted(2,:) ted(3,1) ted(3,3)];
   joe = [ted(1,1) ted(3,2)];

   if any(isfinite(sam)) 
      error(sprintf('%s failed cape code region',fnames{ifile}))
   end
   if ( any(isnan(joe)) || any(isinf(joe)))
      error(sprintf('%s FAILED cape code region',fnames{ifile}))
   end
   
   subplot(3,1,1)
   plot(times,bob(:,1,1), ...
        times,bob(:,3,2) )
%  axis([-Inf Inf 250 302])
   title(fname,'interpreter','none');
   
   disp('getting mountain region ...')
   start = [ 1,400,400];
   endpt = [-1,400,400];
   mtn   = getnc(fname,'tas',start,endpt);
   subplot(3,1,2)
   plot(times, mtn)
   title('mountains')
   if (sum(isfinite(mtn)) ~= length(mtn)) 
      error(sprintf('%s FAILED mountain region %d %d',sum(isfinite(mtn)),length(mtn)))
   end
%  axis([-Inf Inf 250 302])
   
   disp('getting buffalo region ...')
   start = [ 1,456,1103];
   endpt = [-1,458,1103];
   bflo  = getnc(fname,'tas',start,endpt);
   subplot(3,1,3)
   plot(times, bflo )
   title('buffalo')
   if (sum(isfinite(bflo)) ~= length(bflo)) 
      error(sprintf('%s FAILED Buffalo region %d %d',sum(isfinite(bflo)),length(bflo)))
   end
%  axis([-Inf Inf 250 302])

   psfname = sprintf('%s.pdf',experiment);
   print('-dpdf',psfname)

end
