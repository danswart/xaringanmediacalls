#' @title Create List of All Media Calls in an Rmd File
#'
#' @param rmd_file Path and filename of the Rmarkdown file to be processed.  Rmd file must be present in current working directory
#'
#' @return Character vector of all media calls in a markdown file printed to the console
#' @export
#'
#' @importFrom stringr str_extract_all str_detect
#' @importFrom rvest read_html
#' @importFrom xml2 xml_find_all xml_attr
#'

ds_xaringan_media_calls <- function(rmd_file) {

# Read the content of the Rmarkdown file

  rmd_content <- readLines(rmd_file, warn = FALSE)

  # Combine the lines into a single string

  rmd_text <- paste(rmd_content, collapse = "\n")

  # Extract all links using regular expressions

  links1 <- stringr::str_extract_all(rmd_text, "\\bhttps?://\\S+\\b")
  links2 <- stringr::str_extract_all(rmd_text, 'url\\(\\s*[\'"]?([^\\)"\']+)')

  # Combine the links
  all_links <- c(unlist(links1), unlist(links2))


  # Parse the Rmarkdown content as HTML

  rmd_html <- rvest::read_html(rmd_text)

  # Extract links from image tags (img src)

  img_links <- xml2::xml_attr(xml2::xml_find_all(rmd_html, ".//img"), "src")

  # Extract links from anchor tags (a href)

  a_links <- xml2::xml_attr(xml2::xml_find_all(rmd_html, ".//a"), "href")

  # Combine all links

  all_links <- c(all_links, img_links, a_links)

  # Filter the links to get only .gif, .mov, .mp4, .png, .jpg, .jpeg, and .html files

  target_extensions <- c(".gif", ".mov", ".mp4", ".png", ".jpg", ".jpeg", ".html")

  filtered_links <- all_links[stringr::str_detect(all_links, paste0(target_extensions, collapse = "|"))]

  # Print the list of links

  print(unique(filtered_links))

}
