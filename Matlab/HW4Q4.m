GP_datafile = 'GP_data_full.csv';
GEV_datafile = 'max_block_value_full.csv';

GP_data = csvread(GP_datafile);
GEV_data = csvread(GEV_datafile);

GEV_params = gevfit(GEV_data);
GP_params = gpfit(GP_data);
