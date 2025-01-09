#' Setup your catRlog folder system
#'
#' Do this when you are setting up a new project. Run this function with your working directory set to your photo-ID project folder.
#'
#' @return Several folders are created within your photo-ID project directory.
#' @export
#'
setup_project<- function(){

  # Create each folder as long as it doesnt exist yet
  new_dirs <- c('photos', 'photos/photos', 'photos/staged_events',
                'events', 'events/backups',
                'scores', 'scores/score sessions',
                'matches', 'matches/match sessions', 'matches/reviewed matches',
                'catalog', 'catalog/old_keys','catalog/replaced', 'catalog/catalog',
                'print', 'print/front matter', 'print/ID pages',
                'analysis', 'analysis/datasets')

  for(diri in new_dirs){
    if(!dir.exists(diri)){
      dir.create(diri)
    }
  }

}
