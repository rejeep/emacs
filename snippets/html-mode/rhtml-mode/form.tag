#name : form_tag(...)
# --
<% form_tag(${1:url_for_options}, ${2:options}, ${3:*parameters_for_url}) do %>
  $0
<% end %>