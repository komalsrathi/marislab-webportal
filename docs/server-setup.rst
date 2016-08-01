
* Make sure

    - You have root access
    - rhel-7-server-optional-rpms is enabled by setting *enabled = 1* in /etc/yum.repos.d/redhat.repo file
    - 



# site_dir configuration

# Define the location '/NWP'
location /NWP {
  app_dir /srv/shiny-server/neuroblastoma-web-portal;
}

# This assumes a server.R and a ui.R available at /srv/shiny-server/neuroblastoma-web-portal/server.R or /srv/shiny-server/neuroblastoma-web-portal/ui.R

# The shiny application stored in /srv/shiny-server/myApp would be available at http://myserver.org:3838/myApp 
In our case, it would be: http://reslndbhicbio02.research.chop.edu:3838/myApp