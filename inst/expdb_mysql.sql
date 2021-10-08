-- phpMyAdmin SQL Dump
-- version 4.0.10deb1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Nov 03, 2016 at 01:28 PM
-- Server version: 5.5.53-0ubuntu0.14.04.1
-- PHP Version: 5.5.9-1ubuntu4.20

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `expdb`
--

-- --------------------------------------------------------

--
-- Table structure for table `expdb_apsoil`
--

CREATE TABLE IF NOT EXISTS `expdb_apsoil` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `soilid` longtext NOT NULL,
  `country` longtext,
  `site` longtext,
  `region` longtext,
  `soiltype` longtext,
  `nearesttown` longtext,
  `naturalvegetation` longtext,
  `state` longtext,
  `apsoilnumber` int(11) DEFAULT NULL,
  `latitude` double DEFAULT NULL,
  `longitude` double DEFAULT NULL,
  `locationaccuracy` longtext,
  `datasource` longtext,
  `winterdate` longtext,
  `summerdate` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_apsoil_data`
--

CREATE TABLE IF NOT EXISTS `expdb_apsoil_data` (
  `soil_id` int(11) NOT NULL,
  `trait_id` int(11) NOT NULL,
  `value` double NOT NULL,
  PRIMARY KEY (`soil_id`,`trait_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_apsoil_layer`
--

CREATE TABLE IF NOT EXISTS `expdb_apsoil_layer` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `soil_id` int(11) NOT NULL,
  `from_depth` double NOT NULL,
  `to_depth` double NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_apsoil_layer_data`
--

CREATE TABLE IF NOT EXISTS `expdb_apsoil_layer_data` (
  `layer_id` int(11) NOT NULL,
  `trait_id` int(11) NOT NULL,
  `value` double NOT NULL,
  PRIMARY KEY (`layer_id`,`trait_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_fertilization`
--

CREATE TABLE IF NOT EXISTS `expdb_fertilization` (
  `trial_id` int(11) NOT NULL,
  `fertilizer` int(11) NOT NULL,
  `date` date NOT NULL,
  `amount` double NOT NULL,
  PRIMARY KEY (`trial_id`,`fertilizer`,`date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_gene`
--

CREATE TABLE IF NOT EXISTS `expdb_gene` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_gene_allele`
--

CREATE TABLE IF NOT EXISTS `expdb_gene_allele` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `gene_id` int(11) NOT NULL,
  `name` longtext NOT NULL,
  `description` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_genotype`
--

CREATE TABLE IF NOT EXISTS `expdb_genotype` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `breedingline` longtext,
  `alias` longtext,
  `crop` longtext NOT NULL,
  `owner` longtext,
  `releasse` int(11) DEFAULT NULL,
  `maturity` longtext,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=32 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_genotype_gene`
--

CREATE TABLE IF NOT EXISTS `expdb_genotype_gene` (
  `genotype_id` int(11) NOT NULL,
  `allele_id` int(11) NOT NULL,
  PRIMARY KEY (`genotype_id`,`allele_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_irrigation`
--

CREATE TABLE IF NOT EXISTS `expdb_irrigation` (
  `trial_id` int(11) NOT NULL,
  `date` date NOT NULL,
  `amount` double NOT NULL,
  PRIMARY KEY (`trial_id`,`date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_log`
--

CREATE TABLE IF NOT EXISTS `expdb_log` (
  `date` date NOT NULL,
  `comments` longtext NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_met`
--

CREATE TABLE IF NOT EXISTS `expdb_met` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `number` longtext,
  `type` int(11) NOT NULL,
  `latitude` double DEFAULT NULL,
  `longitude` double DEFAULT NULL,
  `file_id` int(11) DEFAULT NULL,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_met_daily`
--

CREATE TABLE IF NOT EXISTS `expdb_met_daily` (
  `met_id` int(11) NOT NULL,
  `year` int(4) NOT NULL,
  `day` int(3) NOT NULL,
  `radn` double NOT NULL,
  `maxt` double NOT NULL,
  `mint` double NOT NULL,
  `rain` double NOT NULL,
  `evap` double NOT NULL,
  `vp` double NOT NULL,
  PRIMARY KEY (`met_id`,`year`,`day`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_node`
--

CREATE TABLE IF NOT EXISTS `expdb_node` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=78 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_node_heritage`
--

CREATE TABLE IF NOT EXISTS `expdb_node_heritage` (
  `node_id` int(11) NOT NULL,
  `parent_node_id` int(11) NOT NULL,
  PRIMARY KEY (`node_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_phenotype`
--

CREATE TABLE IF NOT EXISTS `expdb_phenotype` (
  `plot_id` int(11) NOT NULL,
  `trait_id` int(11) NOT NULL,
  `node_id` int(11) NOT NULL,
  `sample` int(11) NOT NULL,
  `date` date NOT NULL,
  `value` double NOT NULL,
  `quality` int(11) DEFAULT NULL,
  PRIMARY KEY (`plot_id`,`trait_id`,`node_id`,`sample`,`date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_researcher`
--

CREATE TABLE IF NOT EXISTS `expdb_researcher` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `email` longtext,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_site`
--

CREATE TABLE IF NOT EXISTS `expdb_site` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `country` longtext,
  `latitude` double DEFAULT NULL,
  `longitude` double DEFAULT NULL,
  `elevation` double DEFAULT NULL,
  `soil_id` int(11) DEFAULT NULL,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_source`
--

CREATE TABLE IF NOT EXISTS `expdb_source` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `label` longtext,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_trait`
--

CREATE TABLE IF NOT EXISTS `expdb_trait` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `type` longtext NOT NULL,
  `level` longtext NOT NULL,
  `measurement` longtext,
  `measurementindex` int(11) DEFAULT NULL,
  `label` longtext,
  `unit` longtext,
  `description` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=82 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_trial`
--

CREATE TABLE IF NOT EXISTS `expdb_trial` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext NOT NULL,
  `year` int(11) DEFAULT NULL,
  `researcher_id` int(11) DEFAULT NULL,
  `source_id` int(11) DEFAULT NULL,
  `site_id` int(11) DEFAULT NULL,
  `sowing` date NOT NULL,
  `depth` double DEFAULT NULL,
  `density` double DEFAULT NULL,
  `row_spacing` double DEFAULT NULL,
  `met_id` int(11) DEFAULT NULL,
  `notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=5 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_trial_design`
--

CREATE TABLE IF NOT EXISTS `expdb_trial_design` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` char(255) DEFAULT NULL,
  `year` int(11) NOT NULL,
  `site_id` int(11) NOT NULL,
  `trial_id` int(11) NOT NULL,
  `column` int(11) NOT NULL,
  `row` int(11) NOT NULL,
  `replicate` int(11) NOT NULL,
  `treatment` int(11) NOT NULL,
  `block` int(11) DEFAULT NULL,
  `genotype_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=910 ;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_trial_design_extra`
--

CREATE TABLE IF NOT EXISTS `expdb_trial_design_extra` (
  `plot_id` int(11) NOT NULL,
  `name` char(255) NOT NULL,
  `value` longtext NOT NULL,
  PRIMARY KEY (`plot_id`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `expdb_trial_soil`
--

CREATE TABLE IF NOT EXISTS `expdb_trial_soil` (
  `trial_id` int(11) NOT NULL,
  `from_depth` double NOT NULL,
  `to_depth` double NOT NULL,
  `no3` double DEFAULT NULL,
  `nh4` double DEFAULT NULL,
  PRIMARY KEY (`trial_id`,`from_depth`,`to_depth`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
