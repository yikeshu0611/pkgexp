#' Find R File Contain Query Words
#'
#' @param repo repo
#' @param query words
#' @param logical all or any
#' @param local local dir
#'
#' @export
#'
#' @examples
#' find_R('yikeshu0611/set','as.numeric')
find_R <- function(repo,query,logical=c('all','any'),local=NULL){
    logical=match.arg(logical)
    if (is.null(local)){
        #build url and read page
        url=paste0('https://github.com/',repo,'/tree/master/R')
        html=read_html(x = url)
        xpath='//td[@class="content"]//a[@class="js-navigation-open"]'
        #title
        title=html %>%
            html_nodes(xpath = xpath) %>%
            html_attr(name = "title")
        title=paste0(do::Replace0(title,'\\.R'),'.html')
        #creat dir
        dir.repo=strsplit(repo,'/')[[1]]
        for (i in 1:length(dir.repo)) {
            if (i==1) dir.cum='.'
            dir.cum=paste0(dir.cum,'/',dir.repo[i])
            if (!file.exists(dir.cum)) dir.create(dir.cum)
        }
        all.files <- list.files(path = dir.cum)
        if (set::is.sub(title,all.files)){
            k='no need to download'
        }else{
            urlR=html %>%
                html_nodes(xpath = xpath) %>%
                html_attr(name = "href")
            url=paste0('https://github.com',urlR)
            url=url[! title %in% all.files]
            title=title[! title %in% all.files]
            message('\n向目录',dir.cum,'中下载R文件\n')
            for (i in 1:length(url)){
                message(i,'/',length(url),' ',title[i])
                download.file(url[i],paste0(dir.cum,'/',title[i]),quiet = TRUE)
            }
        }
        for (i in 1:length(title)) {
            if (i==1) res=c()
            title.i=paste0(dir.cum,'/',title[i])
            content=read_html(title.i) %>%
                html_nodes(xpath = '//div[@itemprop="text"]')
            for (j in 1:length(query)) {
                if (j==1) check=c()
                check=c(check,grepl(query[j],content))
            }
            check.p=paste0(logical,'(',check,')')
            if (eval(parse(text = check.p))) res=c(res,title[i])
        }
        message('\n')
        res
    }else{
        title=list.files(local)
        title=title[tolower(do::right(title,1))=='r' |
                        tolower(do::right(title,4))=='html']
        for (i in 1:length(title)) {
            if (i==1) res=c()
            title.i=paste0(local,'/',title[i])
            content=read_html(title.i) %>%
                html_nodes(xpath = '//div[@itemprop="text"]')
            for (j in 1:length(query)) {
                if (j==1) check=c()
                check=c(check,grepl(query[j],content))
            }
            check.p=paste0(logical,'(',check,')')
            if (eval(parse(text = check.p))) res=c(res,title[i])
        }
        res
    }
}
