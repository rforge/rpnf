
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>
<p> Download the most <b>recent</b> version of rpnf-package <a href="https://r-forge.r-project.org/R/?group_id=1632"><strong>here</strong></a>. </p>

<h1>Examples</h1>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr>
<td>
<h2>Traditional sample plot </h2>
<pre>
Point & Figure Plot DAX30 (log) 
--------+-----------------------
 7954.90|                      X
 7648.94|                      X
 7354.75|    X X               X
 7071.88|    XOXO            X X
 6799.88|    XOXO            XOX
 6538.35|    XOXO            XOX
 6286.87|X X XO O          X XOX
 6045.07|XOXOX  OX X       XOXOX
 5812.57|XOXOX  OXOXOX     XOXO 
 5589.01| O O   OXOXOXOX X XOX  
 5374.05|       O O OXOXOXOXOX  
 5167.35|           O OXOXOXO   
 4968.61|             OXO O     
 4777.51|             O         
--------+-----------------------
       Y|22222222222222222222222
       Y|00000000000000000000000
       Y|11111111111111111111111
       Y|00000111111111111111122
        |                       
       M|00000000000000000111100
       M|45678338888889999001147
        |                       
       D|00200130111130122000220
       D|57152512015815527461933
</pre>
</td> 
<td>
<h2>Modern style plot</h2>
<img src="sampleplot.png" border="0" alt="Modern style plot sample" />
</td> 
</tr>
</table>

</body>
</html>
