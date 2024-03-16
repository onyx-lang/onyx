#!/bin/sh

# This install script is intended to download and install the latest available
# release of Onyx.
# It attempts to identify the current platform and an error will be thrown if
# the platform is not supported.
#
# Environment variables:
# - ONYX_DIR (optional): defaults to $HOME/.onyx.
#
# You can install using this script:
# $ curl https://raw.githubusercontent.com/onyx-lang/onyx/master/bin/install.sh | sh

# Installer script inspired by:
#  0) https://raw.githubusercontent.com/wasmerio/wasmer-install/master/install.sh
#  1) https://raw.githubusercontent.com/golang/dep/master/install.sh
#  2) https://sh.rustup.rs
#  3) https://yarnpkg.com/install.sh
#  4) https://raw.githubusercontent.com/brainsik/virtualenv-burrito/master/virtualenv-burrito.sh

reset="\033[0m"
red="\033[31m"
green="\033[32m"
yellow="\033[33m"
white="\033[37m"
bold="\033[1m"
dim="\033[2m"

RELEASES_URL="https://github.com/onyx-lang/onyx/releases"

ONYX_VERBOSE="verbose"
if [ -z "$ONYX_INSTALL_LOG" ]; then
  ONYX_INSTALL_LOG="$ONYX_VERBOSE"
fi

onyx_download_json() {
  url="$2"

  # echo "Fetching $url.."
  if test -x "$(command -v curl)"; then
    response=$(curl -s -L -w 'HTTPSTATUS:%{http_code}' -H 'Accept: application/json' "$url")
    body=$(echo "$response" | sed -e 's/HTTPSTATUS\:.*//g')
    code=$(echo "$response" | tr -d '\n' | sed -e 's/.*HTTPSTATUS://')
  elif test -x "$(command -v wget)"; then
    temp=$(mktemp)
    body=$(wget -q --header='Accept: application/json' -O - --server-response "$url" 2>"$temp")
    code=$(awk '/^  HTTP/{print $2}' <"$temp" | tail -1)
    rm "$temp"
  else
    onyx_error "Neither curl nor wget was available to perform http requests"
    return 1
  fi
  if [ "$code" != 200 ]; then
    onyx_error "File download failed with code $code"
    return 1
  fi

  eval "$1='$body'"
  return 0
}

onyx_download_file() {
  url="$1"
  destination="$2"

  # echo "Fetching $url.."
  if test -x "$(command -v curl)"; then
    if [ "$ONYX_INSTALL_LOG" = "$ONYX_VERBOSE" ]; then
      code=$(curl --progress-bar -w '%{http_code}' -L "$url" -o "$destination")
      printf "\033[K\n\033[1A"
    else
      code=$(curl -s -w '%{http_code}' -L "$url" -o "$destination")
    fi
  elif test -x "$(command -v wget)"; then
    if [ "$ONYX_INSTALL_LOG" = "$ONYX_VERBOSE" ]; then
      code=$(wget --show-progress --progress=bar:force:noscroll -q -O "$destination" --server-response "$url" 2>&1 | awk '/^  HTTP/{print $2}' | tail -1)
      printf "\033[K\n\033[1A"
    else
      code=$(wget --quiet -O "$destination" --server-response "$url" 2>&1 | awk '/^  HTTP/{print $2}' | tail -1)
    fi
  else
    onyx_error "Neither curl nor wget was available to perform http requests."
    return 1
  fi

  if [ "$code" = 404 ]; then
    onyx_error "Your platform is not yet supported ($OS-$RUNTIME-$ARCH).$reset\nPlease open an issue on the Onyx repository if you want to use Onyx in your project: https://github.com/onyx-lang/onyx"
    return 1
  elif [ "$code" != 200 ]; then
    onyx_error "File download failed with code $code"
    return 1
  fi
  return 0
}

onyx_detect_profile() {
  if [ -n "${PROFILE}" ] && [ -f "${PROFILE}" ]; then
    echo "${PROFILE}"
    return
  fi

  local DETECTED_PROFILE
  DETECTED_PROFILE=''
  local SHELLTYPE
  SHELLTYPE="$(basename "/$SHELL")"

  if [ "$SHELLTYPE" = "bash" ]; then
    if [ -f "$HOME/.bashrc" ]; then
      DETECTED_PROFILE="$HOME/.bashrc"
    elif [ -f "$HOME/.bash_profile" ]; then
      DETECTED_PROFILE="$HOME/.bash_profile"
    fi
  elif [ "$SHELLTYPE" = "zsh" ]; then
    DETECTED_PROFILE="$HOME/.zshrc"
  elif [ "$SHELLTYPE" = "fish" ]; then
    DETECTED_PROFILE="$HOME/.config/fish/config.fish"
  fi

  if [ -z "$DETECTED_PROFILE" ]; then
    if [ -f "$HOME/.profile" ]; then
      DETECTED_PROFILE="$HOME/.profile"
    elif [ -f "$HOME/.bashrc" ]; then
      DETECTED_PROFILE="$HOME/.bashrc"
    elif [ -f "$HOME/.bash_profile" ]; then
      DETECTED_PROFILE="$HOME/.bash_profile"
    elif [ -f "$HOME/.zshrc" ]; then
      DETECTED_PROFILE="$HOME/.zshrc"
    elif [ -f "$HOME/.config/fish/config.fish" ]; then
      DETECTED_PROFILE="$HOME/.config/fish/config.fish"
    fi
  fi

  if [ ! -z "$DETECTED_PROFILE" ]; then
    echo "$DETECTED_PROFILE"
  fi
}

onyx_link() {

  ONYX_PROFILE="$(onyx_detect_profile)"

  LOAD_STR="\n# Onyx\nexport ONYX_PATH=\"$INSTALL_DIRECTORY\"\n"
  SOURCE_STR="# Onyx config\nexport ONYX_PATH=\"$INSTALL_DIRECTORY\"\nexport PATH=\"\$ONYX_PATH/bin:\$PATH\"\n"

  if [ -z "${ONYX_PROFILE-}" ]; then
    onyx_error "Profile not found. Tried:\n* ${ONYX_PROFILE} (as defined in \$PROFILE)\n* ~/.bashrc\n* ~/.bash_profile\n* ~/.zshrc\n* ~/.profile.\n${reset}Append the following lines to the correct file yourself:\n${SOURCE_STR}"
    return 1
  else
    printf "Updating bash profile $ONYX_PROFILE\n"
    if ! grep -q 'ONYX_PATH' "$ONYX_PROFILE"; then
      command printf "$SOURCE_STR" >>"$ONYX_PROFILE"
      if [ "$ONYX_INSTALL_LOG" = "$ONYX_VERBOSE" ]; then
        printf "we've added the following to your $ONYX_PROFILE\n"
        echo "If you have a different profile please add the following:"
        printf "$dim$SOURCE_STR$reset"
      fi
      onyx_fresh_install=true
    else
      onyx_warning "the profile already has Onyx and has not been changed"
    fi

    version=$($INSTALL_DIRECTORY/bin/onyx version) || (
      onyx_error "Onyx was installed, but doesn't seem to be working. Ensure this binary can be run: $INSTALL_DIRECTORY/bin/onyx"
      return 1
    )

    onyx_install_status "check" "$version installed successfully ✓"

    if [ "$ONYX_INSTALL_LOG" = "$ONYX_VERBOSE" ]; then
      if [ "$onyx_fresh_install" = true ]; then
        printf "onyx will be available the next time you open the terminal.\n"
      fi
    fi
  fi
  return 0
}

initArch() {
  ARCH=$(uname -m)
  case $ARCH in
  amd64) ARCH="amd64" ;;
  x86_64) ARCH="amd64" ;;
  aarch64) ARCH="aarch64" ;;
  arm64) ARCH="arm64" ;; # This is for the macOS M1 ARM chips
  *)
    onyx_error "The system architecture (${ARCH}) is not yet supported by this installation script."
    exit 1
    ;;
  esac
  # echo "ARCH = $ARCH"
}

initOS() {
  OS=$(uname | tr '[:upper:]' '[:lower:]')
  case "$OS" in
  darwin) OS='darwin' ;;
  linux) OS='linux' ;;
  # freebsd) OS='freebsd' ;;
  # mingw*) OS='windows';;
  # msys*) OS='windows';;
  *)
    printf "$red> The OS (${OS}) is not supported by this installation script.$reset\n"
    exit 1
    ;;
  esac
}

initRuntime() {
    printf "${green}Please choose a WebAssembly runtime to use with your Onyx installation.\n${reset}"

    echo "1) Wasmer: An industry standard WebAssembly runtime. Very fast. (default)"
    echo "2) OVM: A custom, lightweight runtime made for Onyx. Supports debugging. Slower than Wasmer."
    echo "3) None: Omit using a runtime and only use Onyx as a compiler to WebAssembly."

    read -p "Which runtime would you like to use? [1/2/3] " selected

    case $selected in
        1) RUNTIME="wasmer"; break ;; 
        2) RUNTIME="ovm"; break ;; 
        3) RUNTIME="none"; break ;; 
        *) echo "Invalid choice. Defaulting to 'wasmer'."; RUNTIME="wasmer" ;;
    esac
}

onyx_install() {
  if [ "$ONYX_INSTALL_LOG" = "$ONYX_VERBOSE" ]; then
    printf "\033[96m                   ######\n"
    printf "\033[96m               ####++++++###\n"
    printf "\033[96m            ###+++++++++++++###\n"
    printf "\033[96m         ###+++++++++++++++++++###\n"
    printf "\033[96m      ###++++++++++++++++++++++++++###\n"
    printf "\033[96m  ###+++++++++++++++++++++++++++++++++###\n"
    printf "\033[96m  #++++++++++++++++++++++++++++++++++++#-#\n"
    printf "\033[96m  #--------------------------------++#####\n"
    printf "\033[96m  #-----------------------------+##+++####\n"
    printf "\033[96m  #--------------------------+##+++++#####      The Onyx Programming Language\n"
    printf "\033[96m  #-----------------------+##+++++++######\n"
    printf "\033[96m  #--------------------##++++++++++#######             Brendan Hansen\n"
    printf "\033[96m  #----------------+##+++++++++++#########\n"
    printf "\033[96m  #-------------+##+++++++++++++##########\n"
    printf "\033[96m  #----------##++++++++++++++++###########\n"
    printf "\033[96m  #------+##++++++++++++++++++############\n"
    printf "\033[96m  #---+##++++++++++++++++++++#############\n"
    printf "\033[96m  ####++++++++++++++++++++++##############\n"
    printf "\033[96m  ###++++++++++++++++++++++##############\n"
    printf "\033[96m     ####+++++++++++++++++############\n"
    printf "\033[96m         ###+++++++++++++##########\n"
    printf "\033[96m            ###+++++++++########\n"
    printf "\033[96m               ####++++######\n"
    printf "\033[96m                   ######\n"
    printf "\n\n"
  else
    printf "The Onyx Programming Language\n\n"
  fi

  onyx_download "$1" "$2" && onyx_link
  onyx_reset
}

onyx_reset() {
  unset -f onyx_install semver_compare onyx_reset onyx_download_json onyx_link onyx_detect_profile onyx_download_file onyx_download onyx_verify_or_quit
}

version() {
  echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'
}

semverParseInto() {
  local RE='v?([0-9]+)[.]([0-9]+)[.]([0-9]+)([.0-9A-Za-z-]*)'

  # # strip word "v" if exists
  # version=$(echo "${1//v/}")

  #MAJOR
  eval $2=$(echo $1 | sed -E "s#$RE#\1#")
  #MINOR
  eval $3=$(echo $1 | sed -E "s#$RE#\2#")
  #MINOR
  eval $4=$(echo $1 | sed -E "s#$RE#\3#")
  #SPECIAL
  eval $5=$(echo $1 | sed -E "s#$RE#\4#")
}

###
# Code taken from 
#    https://raw.githubusercontent.com/wasmerio/wasmer-install/master/install.sh
###
semver_compare() {
  local version_a version_b

  local MAJOR_A=0
  local MINOR_A=0
  local PATCH_A=0
  local SPECIAL_A=0

  local MAJOR_B=0
  local MINOR_B=0
  local PATCH_B=0
  local SPECIAL_B=0

  semverParseInto $1 MAJOR_A MINOR_A PATCH_A SPECIAL_A
  semverParseInto $2 MAJOR_B MINOR_B PATCH_B SPECIAL_B

  # Check if our version is higher
  if [ $MAJOR_A -gt $MAJOR_B ]; then
    echo 1 && return 0
  fi
  if [ $MAJOR_A -eq $MAJOR_B ]; then
    if [ $MINOR_A -gt $MINOR_B ]; then
      echo 1 && return 0
    elif [ $MINOR_A -eq $MINOR_B ]; then
      if [ $PATCH_A -gt $PATCH_B ]; then
        echo 1 && return 0
      elif [ $PATCH_A -eq $PATCH_B ]; then
        if [ -n "$SPECIAL_A" ] && [ -z "$SPECIAL_B" ]; then
          # if the version we're targeting does not have a tag and our current
          # version does, we should upgrade because no tag > tag
          echo -1 && return 0
        elif [ "$SPECIAL_A" \> "$SPECIAL_B" ]; then
          echo 1 && return 0
        elif [ "$SPECIAL_A" = "$SPECIAL_B" ]; then
          # complete match
          echo 0 && return 0
        fi
      fi
    fi
  fi

  # if we're here we know that the target verison cannot be less than or equal to
  # our current version, therefore we upgrade

  echo -1 && return 0
}

onyx_download() {
  # identify platform based on uname output
  initArch || return 1
  initOS || return 1

  if [ ! -z "$2" ]; then
      if [ "$2" = "ovmwasm" ]; then RUNTIME="ovm";
      else                          RUNTIME="$2";
      fi
  else
      initRuntime || return 1;
  fi

  # assemble expected release artifact name
  BINARY="onyx-${OS}-${RUNTIME}-${ARCH}.tar.gz"

  onyx_install_status "downloading" "onyx-$OS-$RUNTIME-$ARCH"
  if [ -z "$1" ]; then
    # The version was not provided, assume latest
    onyx_download_json LATEST_RELEASE "$RELEASES_URL/latest" || return 1
    ONYX_RELEASE_TAG=$(echo "${LATEST_RELEASE}" | tr -s '\n' ' ' | sed 's/.*"tag_name":"//' | sed 's/".*//')
    printf "Latest release: ${ONYX_RELEASE_TAG}\n"
  else
    ONYX_RELEASE_TAG="${1}"
    printf "Installing provided version: ${ONYX_RELEASE_TAG}\n"
  fi

   if which $INSTALL_DIRECTORY/bin/onyx >/dev/null; then
     ONYX_VERSION=$($INSTALL_DIRECTORY/bin/onyx version | sed -n 's/Onyx toolchain version //g;1p')
     printf "Onyx is already installed in ${INSTALL_DIRECTORY} with version: ${ONYX_VERSION}\n"
     ONYX_COMPARE=$(semver_compare $ONYX_VERSION $ONYX_RELEASE_TAG)
     case $ONYX_COMPARE in
     0)
       if [ $# -eq 0 ]; then
         onyx_warning "Onyx is already installed in the latest version: ${ONYX_RELEASE_TAG}"
       else
         onyx_warning "Onyx is already installed with the same version: ${ONYX_RELEASE_TAG}"
       fi
       printf "Do you want to force the installation?"
       onyx_verify_or_quit || return 1
       ;;
     1)
       onyx_warning "The selected version (${ONYX_RELEASE_TAG}) is lower than current installed version ($ONYX_VERSION)"
       printf "Do you want to continue installing Onyx $ONYX_RELEASE_TAG?"
       onyx_verify_or_quit || return 1
       ;;
     -1) ;;
     esac
   fi

  # fetch the real release data to make sure it exists before we attempt a download
  onyx_download_json RELEASE_DATA "$RELEASES_URL/tag/$ONYX_RELEASE_TAG" || return 1

  BINARY_URL="$RELEASES_URL/download/$ONYX_RELEASE_TAG/$BINARY"
  DOWNLOAD_FILE=$(mktemp -t onyx.XXXXXXXXXX)

  printf "Downloading archive from ${BINARY_URL}\n"

  onyx_download_file "$BINARY_URL" "$DOWNLOAD_FILE" || return 1
  printf "\033[K\n\033[1A"

  onyx_install_status "installing" "${INSTALL_DIRECTORY}"

  mkdir -p $INSTALL_DIRECTORY

  # Remove old tests directory that should not be shipped with the toolchain anymore
  rm -rf $INSTALL_DIRECTORY/tests 2>/dev/null

  # Untar the Onyx contents in the install directory
  tar -C $INSTALL_DIRECTORY -zxf $DOWNLOAD_FILE

  # Make the executable executable
  chmod +x $INSTALL_DIRECTORY/bin/onyx
  return 0
}

onyx_error() {
  printf "$bold${red}error${white}: $1${reset}\n"
}

onyx_install_status() {
  printf "$bold${green}${1}${white}: $2${reset}\n"
}

onyx_warning() {
  printf "$bold${yellow}warning${white}: $1${reset}\n"
}

onyx_verify_or_quit() {
  if [ -n "$BASH_VERSION" ]; then
    # If we are in bash, we can use read -n
    read -p "$1 [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
      onyx_error "installation aborted"
      return 1
    fi
    return 0
  fi

  read -p "$1 [y/N]" yn
  case $yn in
  [Yy]*) break ;;
  [Nn]*)
    onyx_error "installation aborted"
    return 1
    ;;
  *) echo "Please answer yes or no." ;;
  esac

  return 0
}

# determine install directory if required
if [ -z "$ONYX_DIR" ]; then
  # If ONYX_DIR is not present
  INSTALL_DIRECTORY="$HOME/.onyx"
else
  # If ONYX_DIR is present
  INSTALL_DIRECTORY="${ONYX_DIR}"
fi

onyx_install "$1" "$2"
