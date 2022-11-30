# New TROLL

In case of a new TROLL version the following changes need to be made to the C++ for use with Rcpp in rcontroll:

-   Adding the Rcpp header

-   Transforming main() in trollCpp() with documentation

-   Replacing the bufi by the Rcpp inputs

-   Removing all the exit()

-   Adding parenthesis to void the corresponding warning

Do not hesitate to add missing changes.

## Rcpp header

    // [[Rcpp::depends(RcppGSL)]]
    #include <Rcpp.h>
    #include <RcppGSL.h>
    using namespace Rcpp; 

## `main()` to `trollCpp()`

    //' @title TROLL simulator
    //'
    //' @description
    //' Wrapper of the TROLL C++ simulator with Rcpp.
    //'
    //' @name trollCpp
    //'
    //' @param global_file char. Path to the global parameters file.
    //' @param climate_file char. Path to the climate file.
    //' @param species_file char. Path to the species file.
    //' @param day_file char. Path to the daytime file.
    //' @param lidar_file char. Path to the lidar file.
    //' @param forest_file char. Path to the forest file.
    //' @param output_file char. Path to the output folder.
    //'
    //' @return Void with outputs files written in the defined folder.
    //'
    //' @examples
    //' \dontrun{
    //' trollCpp(global_file = "test/test_input_global.txt",
    //'          climate_file = "test/test_input_climate.txt",
    //'          species_file = "test/test_input_species.txt",
    //'          day_file = "test/test_input_daily.txt",
    //'          lidar_file = "NULL",
    //'          forest_file = "NULL",
    //'          output_file = "test")
    //' }
    //'
    //' @export
    // [[Rcpp::export]]
    void trollCpp(
        std::string global_file,
        std::string climate_file,
        std::string species_file,
        std::string day_file,
        std::string lidar_file,
        std::string forest_file,
        std::string output_file
    ) {

## bufi

      // From Rcpp acceptable input to TROLL char*
      bufi = &global_file[0] ;
      bufi_climate = &climate_file[0] ;
      bufi_species = &species_file[0] ;
      bufi_daytimevar = &day_file[0] ;
      bufi_data = &forest_file[0] ;
      bufi_pointcloud = &lidar_file[0] ;
      buf = &output_file[0] ;
